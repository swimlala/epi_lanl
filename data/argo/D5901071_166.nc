CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:37Z UW 3.1 conversion   
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  e<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g$   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �D   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �t   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142924  20190522121827  1727_5046_166                   2C  D   APEX                            2143                            040306                          846 @��;�@ 1   @��;��?�@6#n��P�c��`A�71   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`ffBh  Bp  Bw��B�  B�  B�  B�  B�  B�33B�33B�33B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� DfD� D  D� D  Dy�D  D� D   D � D!  D!� D"  D"y�D#  D#� D$  D$�fD%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DI��DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DWfDW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp�fDq  Dq� Dr  Dr� Dr�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`ffBh  Bp  Bw��B�  B�  B�  B�  B�  B�33B�33B�33B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� DfD� D  D� D  Dy�D  D� D   D � D!  D!� D"  D"y�D#  D#� D$  D$�fD%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DI��DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DWfDW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp�fDq  Dq� Dr  Dr� Dr�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�%A�1A�JA�  A���A��A��AоwAк^AиRAд9Aд9Aд9Aв-Aа!Aа!Aа!Aа!Aв-AЮAЧ�AХ�AЧ�AЩ�AЩ�AУ�AЏ\A�bNA��;A�ffA��yA��
AƲ-A��A�{A�=qA���A���A�E�A���A�?}A���A���A��TA�A�A�
=A��wA�33A�JA��A��A���A��jA�  A�ffA���A��-A��mA�;dA���A��A�M�A�{A��RA�K�A���A�n�A��
A�^5A� �A��A�~�A�A�~�A�oA�E�A�C�A��#A���A��-A�ƨA���A�VA�O�A��9A�I�A��A�K�A��-A�%A��DA�bNA�bNA�I�A�
=A��;A���A���A��!A���A�7LA�ĜA�"�A�ffA��+A�oA��DA�l�A��A��yA��
A�5?A�%A��jA�G�A��yA�33A��A��A��-A��yA�+A�z�A�?}A�E�A���A��A�  A�r�A��RA�?}A�hsA|��Aw�mAr��ApI�Ao�Ao7LAn��Al�HAjĜAi�mAh��Agt�Ae7LAc�;Ac�Aa��A_��A^E�A]x�A\5?AYdZAX�jAWhsAUoAR�AQ"�AN��AN{AK?}AI33AHjAG��AG��AF��ADn�ABM�A@�9A?�A>n�A<-A;�A:��A:ffA9|�A8�yA7�A7\)A6�yA4�A41'A4bA3A3ƨA3��A2z�A0z�A.��A*�HA(��A(��A'33A&9XA$��A#O�A"�uA!%A �uA 5?Al�A�uA�hA�AA�A-A�AoA�HA��A�A=qA�A1'A�^A�A�!A1'A��A��A �A\)AVA"�AJAhsAȴA �Ax�A
=A
VA��AA�A��A�jA-A��A��AA�`AG�A �A �/A �jA �jA ��A  �@�^5@�@�@���@���@�hs@�bN@���@�Q�@�~�@��m@�X@�P@홚@��H@�@��m@�v�@�D@�!@�%@�{@�+@�E�@؃@�X@�j@�+@��@�M�@͙�@�O�@��`@�j@�Q�@��;@��H@ɺ^@��/@ȃ@�z�@�I�@�t�@Ɵ�@���@ũ�@Ł@�O�@��@���@Ĭ@��
@���@�n�@�-@��^@���@�hs@�V@+@��H@��y@�33@�@���@�G�@�z�@��@�S�@�"�@�o@�
=@�+@��!@�/@���@�"�@��@�~�@�$�@�J@��@�-@�5?@�-@�@��h@���@�@��h@���@���@�+@�j@�r�@� �@�dZ@�^5@�J@���@�/@��R@��^@�&�@�Z@�o@���@�  @�ƨ@�K�@�33@��@��@�o@�+@�"�@��y@���@���@�E�@�ȴ@�J@�Q�@�  @��@�\)@�;d@��@�@���@�J@���@���@���@���@��7@��7@��7@��@�x�@�p�@�p�@�p�@�hs@�X@�hs@�hs@��@�7L@���@���@���@��h@��7@�x�@�Ĝ@�b@���@�\)@�\)@�\)@�\)@�o@��H@��R@���@��\@��+@�v�@�n�@�^5@�E�@�-@�J@���@��#@���@�hs@���@�r�@��;@�t�@�C�@�"�@�o@��y@���@��+@��@�J@�X@��`@��@�  @���@�ƨ@�"�@�ff@�@�X@�&�@��`@���@�j@�Z@�1'@�S�@�\)@�C�@�S�@�dZ@�;d@�33@��@�ȴ@�n�@�{@���@�r�@��@�S�@�33@��@�o@��@��y@�V@�@���@��`@��@��@�1'@��m@��@��@�@���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�%A�1A�JA�  A���A��A��AоwAк^AиRAд9Aд9Aд9Aв-Aа!Aа!Aа!Aа!Aв-AЮAЧ�AХ�AЧ�AЩ�AЩ�AУ�AЏ\A�bNA��;A�ffA��yA��
AƲ-A��A�{A�=qA���A���A�E�A���A�?}A���A���A��TA�A�A�
=A��wA�33A�JA��A��A���A��jA�  A�ffA���A��-A��mA�;dA���A��A�M�A�{A��RA�K�A���A�n�A��
A�^5A� �A��A�~�A�A�~�A�oA�E�A�C�A��#A���A��-A�ƨA���A�VA�O�A��9A�I�A��A�K�A��-A�%A��DA�bNA�bNA�I�A�
=A��;A���A���A��!A���A�7LA�ĜA�"�A�ffA��+A�oA��DA�l�A��A��yA��
A�5?A�%A��jA�G�A��yA�33A��A��A��-A��yA�+A�z�A�?}A�E�A���A��A�  A�r�A��RA�?}A�hsA|��Aw�mAr��ApI�Ao�Ao7LAn��Al�HAjĜAi�mAh��Agt�Ae7LAc�;Ac�Aa��A_��A^E�A]x�A\5?AYdZAX�jAWhsAUoAR�AQ"�AN��AN{AK?}AI33AHjAG��AG��AF��ADn�ABM�A@�9A?�A>n�A<-A;�A:��A:ffA9|�A8�yA7�A7\)A6�yA4�A41'A4bA3A3ƨA3��A2z�A0z�A.��A*�HA(��A(��A'33A&9XA$��A#O�A"�uA!%A �uA 5?Al�A�uA�hA�AA�A-A�AoA�HA��A�A=qA�A1'A�^A�A�!A1'A��A��A �A\)AVA"�AJAhsAȴA �Ax�A
=A
VA��AA�A��A�jA-A��A��AA�`AG�A �A �/A �jA �jA ��A  �@�^5@�@�@���@���@�hs@�bN@���@�Q�@�~�@��m@�X@�P@홚@��H@�@��m@�v�@�D@�!@�%@�{@�+@�E�@؃@�X@�j@�+@��@�M�@͙�@�O�@��`@�j@�Q�@��;@��H@ɺ^@��/@ȃ@�z�@�I�@�t�@Ɵ�@���@ũ�@Ł@�O�@��@���@Ĭ@��
@���@�n�@�-@��^@���@�hs@�V@+@��H@��y@�33@�@���@�G�@�z�@��@�S�@�"�@�o@�
=@�+@��!@�/@���@�"�@��@�~�@�$�@�J@��@�-@�5?@�-@�@��h@���@�@��h@���@���@�+@�j@�r�@� �@�dZ@�^5@�J@���@�/@��R@��^@�&�@�Z@�o@���@�  @�ƨ@�K�@�33@��@��@�o@�+@�"�@��y@���@���@�E�@�ȴ@�J@�Q�@�  @��@�\)@�;d@��@�@���@�J@���@���@���@���@��7@��7@��7@��@�x�@�p�@�p�@�p�@�hs@�X@�hs@�hs@��@�7L@���@���@���@��h@��7@�x�@�Ĝ@�b@���@�\)@�\)@�\)@�\)@�o@��H@��R@���@��\@��+@�v�@�n�@�^5@�E�@�-@�J@���@��#@���@�hs@���@�r�@��;@�t�@�C�@�"�@�o@��y@���@��+@��@�J@�X@��`@��@�  @���@�ƨ@�"�@�ff@�@�X@�&�@��`@���@�j@�Z@�1'@�S�@�\)@�C�@�S�@�dZ@�;d@�33@��@�ȴ@�n�@�{@���@�r�@��@�S�@�33@��@�o@��@��y@�V@�@���@��`@��@��@�1'@��m@��@��@�@���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBhsBhsBhsBhsBiyBiyBiyBjBjBjBk�Bl�Bl�Bm�Bm�Bm�Bn�Bo�Bp�Bp�Bp�Bq�Br�Bs�Bt�Bu�Bt�Bn�BG�B8RB1'B+B-B0!B8RB>wB@�B@�BE�BN�Bl�B�B}�Bt�Bl�Bk�Bm�Bm�Bn�Bn�Bq�Bt�Bs�B�+B�PB�bB�uB�{B�hB��B��B�B�B�B�B�B�LB�^B�dB�RB�LB�LB�FB�3B�B�B��B��B��B��B�B�B�B��B�uB�PB�DB�7B�Bz�B�B�=B� Bp�Bk�BaHB\)BP�BB�B+B"�B�B+B  B��B�B�B�B�mB�B��B��B�B��B�+B� Bq�BYBO�B<jB/B&�B�B
��B
�5B
ŢB
�?B
��B
y�B
`BB
F�B
5?B
bB	�B	��B	��B	�dB	�LB	�'B	��B	��B	�bB	�=B	�B	u�B	l�B	gmB	cTB	\)B	VB	R�B	I�B	M�B	XB	K�B	=qB	1'B	%�B	�B	�B	
=B��B		7B	PB	uB	uB	+B��B��B�B�mB�;B�B�
B�B�#B�HB�ZB�fB�ZB��B��B��B��B��BɺB�jB��B��B��B��B��B��B��B�uB�PB�=B�7B�1B�+B�%B�B�B�B�B�B� B� B}�B|�Bx�Bw�Bu�Bt�Br�Br�Bq�Bo�Bn�Bm�Bl�Bk�BiyBl�BgmBe`BdZBdZBcTBaHB_;B`BBbNBk�BhsBiyBl�Bk�BhsBhsBffBffBffBffBffBe`BdZBgmBhsBiyBiyBjBz�B{�By�Bv�Bt�Bq�Bo�Bo�Bn�Bp�Br�Bu�Bu�Bu�Bs�Bo�Bk�Bu�B|�B|�B� B�B�B�JB�hB�uB�{B�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�'B�-B�-B�RB�qB�wB��BĜBƨB��B�B�#B�5B�5B�HB�`B�B�B�B�B�B�B�B�B�B��B�B��B��B��B��B	B	+B	DB	\B	hB	�B	�B	�B	�B	!�B	+B	.B	0!B	-B	,B	33B	6FB	;dB	?}B	?}B	>wB	=qB	<jB	;dB	:^B	9XB	7LB	:^B	<jB	=qB	C�B	F�B	G�B	H�B	J�B	K�B	M�B	O�B	W
B	\)B	`BB	ffB	gmB	l�B	m�B	m�B	l�B	l�B	l�B	m�B	w�B	�B	�B	�%B	�%B	�+B	�1B	�7B	�7B	�=B	�DB	�JB	�PB	�PB	�PB	�\B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�3B	�3B	�9B	�?B	�?B	�FB	�LB	�RB	�RB	�XB	�^B	�^B	�dB	�jB	�qB	�wB	��B	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ĜB	ĜB	ÖB	ÖB	B	ÖB	ÖB	ÖB	ĜB	ƨB	ǮB	ǮB	ǮB	ƨB	ƨB	ǮB	ȴB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BhsBhsBhsBhsBiyBiyBjBjBjBjBk�Bl�Bl�Bm�Bm�Bm�Bn�Bo�Bp�Bp�Bp�Bq�Br�Bs�Bt�Bu�Bt�Bs�BK�B=qB<jB0!B1'B8RB<jB?}BA�BB�BF�BO�Bn�B�B�Bw�Bm�Bl�Bo�Bn�Bo�Bp�Bs�Bu�Bv�B�=B�bB�{B��B��B�{B��B��B�B�!B�'B�B�!B�^B�jB�jB�XB�XB�^B�XB�FB�-B�'B��B��B��B��B�B�3B�!B��B��B�\B�\B�JB�+B|�B�B�VB�Bq�Bp�BffB`BBW
BL�B-B%�B#�BDBB��B��B�B�B�B�;B��BȴB�LB��B�7B�Bz�B\)BVB?}B2-B)�B�B+B
�`B
ɺB
�wB
��B
�B
hsB
K�B
A�B
�B
  B	�
B	B	�jB	�XB	�FB	�B	��B	�{B	�VB	�7B	y�B	n�B	iyB	gmB	_;B	XB	VB	O�B	O�B	[#B	P�B	B�B	6FB	,B	�B	�B	\B��B	DB	PB	�B	�B	VB	B��B�B�B�NB�B�B�B�/B�ZB�fB�sB�B�
B��B��B��B��B��BB�!B��B��B��B��B��B��B��B�\B�VB�DB�7B�7B�1B�%B�B�B�B�B�B�B~�B� B|�Bz�Bx�Bv�Bt�Bs�Bs�Bq�Bq�Bo�Bo�Bo�Bm�Bo�BiyBgmBffBffBe`BdZBcTBbNBdZBn�BjBk�Bp�Bn�Bk�Bm�BgmBffBgmBffBgmBgmBgmBhsBiyBiyBiyBk�B|�B~�B}�By�By�Bu�Br�Br�Br�Br�Bu�Bx�Bu�Bx�Bv�Bt�Bp�Bw�B� B�B�B�B�%B�VB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�-B�'B�3B�9B�^B�wB�}BBŢBŢB��B�B�B�5B�5B�NB�`B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	  B	B	+B	DB	\B	hB	�B	�B	�B	�B	"�B	,B	.B	33B	1'B	,B	49B	7LB	<jB	@�B	?}B	?}B	@�B	=qB	<jB	;dB	;dB	:^B	;dB	<jB	>wB	C�B	F�B	G�B	H�B	J�B	K�B	M�B	O�B	XB	\)B	`BB	gmB	iyB	l�B	m�B	m�B	l�B	l�B	l�B	m�B	x�B	�B	�B	�%B	�%B	�+B	�1B	�7B	�7B	�=B	�DB	�JB	�PB	�PB	�PB	�\B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�3B	�3B	�9B	�?B	�?B	�FB	�LB	�RB	�RB	�XB	�^B	�^B	�dB	�qB	�wB	�}B	B	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ŢB	ĜB	ĜB	B	ÖB	ĜB	ĜB	ŢB	ǮB	ǮB	ȴB	ȴB	ƨB	ƨB	ȴB	��B	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<e`B<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447322012010314473220120103144732  AO  ARGQ                                                                        20111130142924  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142924  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144732  IP                  G�O�G�O�G�O�                