CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:20Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               iA   AO  20111130141540  20190522121826  1727_5046_105                   2C  D   APEX                            2143                            040306                          846 @ԝh�K�1   @ԝi� @6V�+J�c�r� Ĝ1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<�fD=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dy� D�3D�p D�� D���D�33D�L�D��3D��3D�0 D�` D���D�� D�33D�VfDڦfD��fD�  D�` D� D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<�fD=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dy� D�3D�p D�� D���D�33D�L�D��3D��3D�0 D�` D���D�� D�33D�VfDڦfD��fD�  D�` D� D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�(�A�?}A�A�A�A�A�C�A�E�A�E�A�E�A�G�A�G�A�E�A�G�A�G�A�E�A�C�A�?}A�;dA�7LA� �A��A� �A��A� �A� �A��A�{A���A�%A�%A���A��/AΩ�A΋DA�?}A�n�A�A��A�|�A��
A�+A��A���A�I�A���A�A��A�?}A�oA�C�A��HA�l�A��A��jA���A�$�A�r�A�O�A��A�E�A�bA�^5A�\)A���A�G�A�G�A���A�S�A���A�33A��A��\A��jA�p�A�x�A���A��PA�G�A�ĜA� �A�r�A��A�%A�x�A�"�A���A�jA��FA�VA�I�A�ĜA��/A�K�A��TA�C�A�A��DA�-A�A���A��
A��yA�l�A�{A��uA���A��A���A���A�{A�~�A��9A��hA��/A�n�A|��At��As33Aq`BAnZAk��Ai��Ag
=Ad��Ae�Ac��Ab �Ab�Aa�Aa�Aa��A`~�A]�
A]K�A\jA[XAY�AWVAU"�AT�ATZAT�ASoARjAR �AP�AO��AN�AMƨAMK�ALn�AI"�AF �ACO�AB�jAA�A@��A@ �A?�FA?|�A?�A<�A<-A;t�A:ȴA:~�A9�hA8E�A7��A7S�A7K�A7+A6��A4�yA2��A1��A0jA.�A-�wA,{A+&�A*��A*^5A)��A)?}A(I�A'�-A&��A%"�A$I�A#33A!\)A ��A ��Ap�A�\AQ�A1A��AO�A�jA�A�FAM�A��A��A=qA�A�TAK�A�AĜAJAS�AĜA��AffA��A`BA
VA	�A	�-A�AZA��A�`A(�AXA�
A&�A Z@���@�-@�`B@���@��/@���@�-@���@��@��-@��/@��m@�C�@�J@���@� �@��@���@���@ߕ�@�33@�
=@���@�5?@�9X@�5?@ٲ-@ّh@�dZ@�V@�-@��#@Չ7@���@�Z@��;@�t�@�dZ@�33@��y@ҧ�@�n�@�J@�x�@��@��`@��@ɺ^@� �@�=q@�%@��
@�`B@�ƨ@�X@�I�@��
@�p�@���@��@���@���@���@�1@�C�@��T@�Z@��
@�K�@���@�E�@���@�
=@�=q@���@�hs@���@��
@��@�v�@�@�/@�Ĝ@�(�@���@�K�@�33@�o@�~�@�@��T@��^@���@��@�%@��@�1'@��
@���@��@��@�dZ@��!@��#@�A�@��F@��@��@��@��@��F@���@��
@�I�@��9@��u@���@���@���@��u@�(�@��;@�l�@�33@�"�@�
=@�@�@�@�
=@�o@�o@�o@��y@��H@�ȴ@���@��R@�$�@�7L@��u@�Q�@��H@�C�@��@�b@� �@���@��\@��T@��T@�5?@�ff@���@���@�@�n�@���@���@��7@��@��9@�z�@�Z@��D@�(�@��+@�(�@��@��F@�Ĝ@��@�\)@�=q@��D@��j@�I�@��F@��R@��!@�ff@�@�$�@�;d@��w@�j@��@�@��^@�%@�(�@�ƨ@�ff@���@��@��#@��`@��@��m@��D@��@���@��R@��y@�S�@�J@�@�-@�ff@��!@�+@�|�@���@��H@���@�+@�n�@��^@�O�@�G�@�G�@���@��u@�j@�I�@��@��
@�
=@���@�+@���@�$�@��^@��@�hs@�O�@��@��@��/@��/@��j@��u@�Z@��@��@~��@~@}�-@}`B@}?}@}?}@}�@|�j@|�j@|�@|�D@|j@{C�@tj@l1@`��@YG�@Nff@IX@B~�@;ƨ@5��@0��@,I�@'+@!G�@�@��@v�@�m@
�H@ȴ@ r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�(�A�?}A�A�A�A�A�C�A�E�A�E�A�E�A�G�A�G�A�E�A�G�A�G�A�E�A�C�A�?}A�;dA�7LA� �A��A� �A��A� �A� �A��A�{A���A�%A�%A���A��/AΩ�A΋DA�?}A�n�A�A��A�|�A��
A�+A��A���A�I�A���A�A��A�?}A�oA�C�A��HA�l�A��A��jA���A�$�A�r�A�O�A��A�E�A�bA�^5A�\)A���A�G�A�G�A���A�S�A���A�33A��A��\A��jA�p�A�x�A���A��PA�G�A�ĜA� �A�r�A��A�%A�x�A�"�A���A�jA��FA�VA�I�A�ĜA��/A�K�A��TA�C�A�A��DA�-A�A���A��
A��yA�l�A�{A��uA���A��A���A���A�{A�~�A��9A��hA��/A�n�A|��At��As33Aq`BAnZAk��Ai��Ag
=Ad��Ae�Ac��Ab �Ab�Aa�Aa�Aa��A`~�A]�
A]K�A\jA[XAY�AWVAU"�AT�ATZAT�ASoARjAR �AP�AO��AN�AMƨAMK�ALn�AI"�AF �ACO�AB�jAA�A@��A@ �A?�FA?|�A?�A<�A<-A;t�A:ȴA:~�A9�hA8E�A7��A7S�A7K�A7+A6��A4�yA2��A1��A0jA.�A-�wA,{A+&�A*��A*^5A)��A)?}A(I�A'�-A&��A%"�A$I�A#33A!\)A ��A ��Ap�A�\AQ�A1A��AO�A�jA�A�FAM�A��A��A=qA�A�TAK�A�AĜAJAS�AĜA��AffA��A`BA
VA	�A	�-A�AZA��A�`A(�AXA�
A&�A Z@���@�-@�`B@���@��/@���@�-@���@��@��-@��/@��m@�C�@�J@���@� �@��@���@���@ߕ�@�33@�
=@���@�5?@�9X@�5?@ٲ-@ّh@�dZ@�V@�-@��#@Չ7@���@�Z@��;@�t�@�dZ@�33@��y@ҧ�@�n�@�J@�x�@��@��`@��@ɺ^@� �@�=q@�%@��
@�`B@�ƨ@�X@�I�@��
@�p�@���@��@���@���@���@�1@�C�@��T@�Z@��
@�K�@���@�E�@���@�
=@�=q@���@�hs@���@��
@��@�v�@�@�/@�Ĝ@�(�@���@�K�@�33@�o@�~�@�@��T@��^@���@��@�%@��@�1'@��
@���@��@��@�dZ@��!@��#@�A�@��F@��@��@��@��@��F@���@��
@�I�@��9@��u@���@���@���@��u@�(�@��;@�l�@�33@�"�@�
=@�@�@�@�
=@�o@�o@�o@��y@��H@�ȴ@���@��R@�$�@�7L@��u@�Q�@��H@�C�@��@�b@� �@���@��\@��T@��T@�5?@�ff@���@���@�@�n�@���@���@��7@��@��9@�z�@�Z@��D@�(�@��+@�(�@��@��F@�Ĝ@��@�\)@�=q@��D@��j@�I�@��F@��R@��!@�ff@�@�$�@�;d@��w@�j@��@�@��^@�%@�(�@�ƨ@�ff@���@��@��#@��`@��@��m@��D@��@���@��R@��y@�S�@�J@�@�-@�ff@��!@�+@�|�@���@��H@���@�+@�n�@��^@�O�@�G�@�G�@���@��u@�j@�I�@��@��
@�
=@���@�+@���@�$�@��^@��@�hs@�O�@��@��@��/@��/@��j@��u@�Z@��@��@~��@~@}�-@}`B@}?}@}?}@}�@|�j@|�j@|�@|�D@|j@{C�@tj@l1@`��@YG�@Nff@IX@B~�@;ƨ@5��@0��@,I�@'+@!G�@�@��@v�@�m@
�H@ȴ@ r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�oB�hB�hB�hB�hB�hB�hB�hB�hB�hB�oB�hB�oB�oB�uB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B�+B'�B��B��B��B��B��B�5BB�B�B(�B33Bm�B�VB��B�^B�}B�qB��B��B��B�wB�wB�}B�wB�wB�}B�}B�qB�9B�B�B��B��B��B��B�PB}�Bk�B_;B]/B[#BR�BI�B>wB-B!�B�B{BhBJBB��B�B�HB��BǮBB�RB��B�1B�B|�BffBQ�BE�B>wB0!B&�B�B	7B
��B
��B
�B
�wB
�B
��B
w�B
Q�B
\B	��B	��B	�LB	��B	�\B	�B	w�B	q�B	�VB	��B	�JB	�{B	��B	��B	��B	�hB	|�B	s�B	ffB	W
B	H�B	F�B	B�B	=qB	;dB	9XB	@�B	G�B	E�B	;dB	33B	0!B	-B	(�B	!�B		7B�B�TB�;B�)B�
B��B��B��B��BB�wB�dB�XB�LB�9B�3B�!B�!B�!B�B�B��B��B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�VB�JB�=B�7B�1B�1B�%B�B�B~�Bz�By�Bw�Bv�Bu�Bs�Bt�Br�Bq�Bp�Bo�Bn�Bk�BiyBhsBe`BdZBcTBcTBaHB^5B^5B\)B[#BYBW
BVBW
BYB\)B[#BYB[#B[#B[#BZBXBW
BW
BVBT�BS�BR�BR�BP�BP�BN�BP�BQ�BQ�BQ�BP�BP�BP�BO�BO�BN�BL�BL�BL�BK�BK�BK�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BI�BH�BF�BK�BL�BM�BM�BM�BT�BW
BVBS�BQ�BM�BI�BF�BC�BC�BA�B@�B?}B>wB?}BB�BJ�BJ�BK�BJ�BJ�BM�BS�B\)B_;BaHBbNBe`BhsBiyBo�Bt�Bw�By�B{�B~�B� B�B�B�B�1B�=B�JB�JB�PB�\B�hB�{B��B��B��B��B��B��B��B�FB�dB�jB�jB�jB�qB�qB�}BŢB��B�)B�NB�NB�ZB�`B�fB�B�B��B��B��B��B��B	B	B	B	+B		7B	DB	VB	bB	oB	{B	�B	�B	�B	�B	�B	�B	%�B	)�B	-B	-B	-B	.B	.B	0!B	49B	8RB	<jB	@�B	C�B	J�B	M�B	M�B	N�B	N�B	N�B	O�B	P�B	T�B	Q�B	J�B	D�B	E�B	J�B	P�B	VB	S�B	Q�B	Q�B	T�B	S�B	S�B	T�B	XB	XB	\)B	cTB	l�B	o�B	u�B	z�B	}�B	|�B	z�B	x�B	w�B	r�B	t�B	u�B	� B	�B	� B	�B	�%B	�+B	�%B	�+B	�1B	�JB	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�-B	�-B	�'B	�9B	�^B	�qB	��B	��B	��B	B	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�5B	�5B	�;B	�BB	�mB	��B
JB
�B
$�B
,B
33B
9XB
>wB
D�B
H�B
M�B
S�B
YB
_;B
e`B
gmB
iyB
r�B
x�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�oB�hB�hB�hB�hB�hB�hB�hB�hB�hB�oB�hB�oB�oB�uB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�B�B�B�VB2-B�HB�B�BŢB�
B�ZB%B�B�B)�B33Bp�B�\B��B�qBĜB��BŢBÖB��B��B��B��B��BÖBBBB�^B�!B�B�B�B��B��B�{B�Bo�B`BB_;B^5BW
BN�BG�B33B$�B�B�BuB\BB��B�B�`B��BɺBŢB�wB�!B�=B�B�Bl�BVBH�BE�B33B+B"�BVB
��B
��B
�NB
ÖB
�'B
��B
~�B
YB
�B	�
B	��B	�jB	��B	�oB	�DB	|�B	q�B	�hB	��B	�JB	�{B	��B	��B	��B	��B	~�B	u�B	iyB	[#B	O�B	K�B	D�B	>wB	<jB	<jB	B�B	H�B	H�B	>wB	5?B	33B	.B	,B	+B	hB��B�`B�NB�;B�B��B��B��B��BŢB��B�qB�^B�^B�RB�?B�'B�!B�'B�!B�'B�B�B�!B�-B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B�hB�\B�bB�PB�=B�7B�7B�+B�+B�+B�B~�Bz�Bz�Bx�Bw�Bx�Bv�Bs�Br�Bs�Br�Bp�Bo�Bm�Bk�Bl�BhsBe`BdZBe`BffBaHB_;B^5B\)B\)BYBZB]/B]/B]/B^5B\)B]/B]/B\)B]/B[#BXBW
BVBVBT�BS�BR�BS�BS�BS�BR�BQ�BR�BQ�BS�BS�BP�BP�BR�BN�BL�BM�BL�BL�BK�BK�BK�BJ�BK�BK�BK�BK�BJ�BJ�BL�BM�BO�BO�BP�BQ�BW
BZB[#BW
BVBO�BJ�BJ�BF�BD�BB�BA�B?}B?}B@�BD�BL�BK�BL�BK�BK�BO�BVB]/B`BBbNBcTBffBiyBk�Bp�Bu�Bx�Bz�B|�B� B� B�B�B�%B�1B�=B�JB�JB�VB�bB�oB��B��B��B��B��B��B��B��B�LB�dB�jB�jB�jB�qB�qB�}BĜB��B�)B�NB�NB�ZB�`B�mB�B�B��B��B��B��B��B	B	B	B	+B		7B	DB	VB	bB	oB	{B	�B	�B	�B	�B	�B	�B	$�B	)�B	-B	.B	/B	/B	.B	0!B	49B	8RB	<jB	@�B	D�B	K�B	M�B	N�B	O�B	N�B	N�B	O�B	P�B	VB	T�B	N�B	E�B	E�B	H�B	P�B	YB	VB	T�B	Q�B	VB	T�B	T�B	T�B	XB	YB	\)B	bNB	k�B	n�B	t�B	y�B	}�B	}�B	{�B	y�B	y�B	s�B	u�B	t�B	�B	�B	� B	�B	�1B	�1B	�%B	�+B	�1B	�VB	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�-B	�3B	�-B	�9B	�^B	�wB	��B	��B	��B	B	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�5B	�5B	�;B	�BB	�mB	��B
JB
�B
$�B
,B
33B
9XB
>wB
D�B
H�B
M�B
S�B
YB
_;B
e`B
gmB
iyB
r�B
x�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447102012010314471020120103144710  AO  ARGQ                                                                        20111130141540  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141540  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144710  IP                  G�O�G�O�G�O�                