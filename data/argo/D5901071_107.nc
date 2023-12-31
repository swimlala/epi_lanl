CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:21Z UW 3.1 conversion   
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ed   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  gL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               kA   AO  20111130141608  20190522121826  1727_5046_107                   2C  D   APEX                            2143                            040306                          846 @ԟ��G@ 1   @ԟ����@6S�����c��S��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B�33B�  B�  B�  B���B���B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B�33B�  B�  B�  B���B���B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�x�A�p�A�dZA�l�A�ffA�M�A�I�A�E�A�A�A�=qA�=qA�9XA�7LA�5?A�-A�/A�1'A�1'A�/A�/A�-A�$�A�$�A�&�A�&�A�(�A�&�A�"�A�$�A�"�A�"�A�"�A�"�A�"�A�$�A�"�A�
=A̮A��AƑhAå�A��DA�z�A���A��-A��DA�M�A�M�A��!A�33A�  A��A�&�A��A�O�A���A���A��uA�7LA��A��^A�^5A�&�A�1A��A�x�A���A�?}A�1A�  A�n�A���A��uA�XA��!A���A�I�A���A�%A�9XA���A�O�A���A�l�A��A��wA���A�~�A�&�A��A���A�Q�A���A��HA�l�A���A���A��uA�1A�E�A�z�A�M�A�(�A���A��A�M�A�?}A� �A�=qA�{A�oA���A�`BA��A��A�ƨA���A��A~��A|��AwO�As�^Ap�/Aj�HAe�PAeO�Ad��A`ȴA_��A\�HA\��A\�DA[�mA[l�AZ��AZ�`AZ$�AXz�AVjAV�9AVZAUO�AS�AShsAR��AR�yAR��AQ�AO�7AN{ALJAK33AJ�AJr�AI�AIO�AH��AHr�ADAC�
AC"�AAl�A?A>z�A>�!A=��A9A7�#A7�hA7G�A6�\A6(�A5�7A3�wA2�A2�/A2(�A1%A/��A.��A-��A-A-�A,�\A,1A+dZA+
=A*z�A*1'A*1'A)��A)
=A'��A&�HA&^5A%�#A%t�A$��A$�uA$=qA"�HA!�A!?}AhsAȴA��AffA  A$�A1'A&�Az�AAC�AbNA"�A�A�RA
=AȴA
(�Az�AA��A+AQ�A�FA�hA �A&�A ��A �@�;d@��!@�-@���@��/@� �@��@�o@���@�dZ@�@�33@���@홚@�dZ@�Ĝ@旍@�?}@⟾@�O�@��m@ߕ�@߅@�;d@�@��@�@���@�~�@�%@�b@ۥ�@ڟ�@��@�Z@׶F@���@��H@��@�9X@�r�@���@��/@��H@�^5@��^@���@�b@��@�n�@�5?@��@���@��7@�?}@���@��u@�9X@��F@�5?@�I�@���@�o@�ȴ@���@���@�v�@�ff@�=q@�$�@�p�@��;@�|�@�@��@��@��/@��u@�Q�@�1@��
@���@�|�@�C�@���@��#@���@���@��@��@��7@�G�@�7L@��/@��F@�33@�C�@�5?@��7@�hs@�hs@�`B@�x�@��-@�@���@��T@��@���@�$�@�v�@��R@�@�t�@�|�@��@��y@��\@�=q@�M�@�^5@��+@�ȴ@��y@���@��R@�@�o@�@���@�v�@��@�9X@��F@��
@��P@���@��\@�5?@�E�@�V@���@��@��@�|�@�b@�j@�bN@�  @��@�ƨ@���@�|�@��@�
=@��@�J@�x�@�&�@���@���@�1'@�  @��w@��@�C�@��y@��@��@�ȴ@���@�ȴ@���@�^5@�/@���@�z�@���@��@�E�@�@�33@���@��-@��T@�5?@��@���@��/@�j@�A�@�  @��j@�G�@��#@�5?@��-@��u@�ƨ@��;@�(�@�9X@�Z@���@�Ĝ@�%@�V@���@�Ĝ@��j@�j@���@�\)@�+@��@�=q@��@�\)@�@��7@�V@��@�z�@�r�@��u@��`@���@�V@�7L@�x�@�7L@��/@���@�Ĝ@��@�9X@��;@�|�@�dZ@�C�@��y@���@�ff@�@���@�x�@�hs@�`B@�hs@�7L@��`@��D@�z�@��u@�z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�x�A�p�A�dZA�l�A�ffA�M�A�I�A�E�A�A�A�=qA�=qA�9XA�7LA�5?A�-A�/A�1'A�1'A�/A�/A�-A�$�A�$�A�&�A�&�A�(�A�&�A�"�A�$�A�"�A�"�A�"�A�"�A�"�A�$�A�"�A�
=A̮A��AƑhAå�A��DA�z�A���A��-A��DA�M�A�M�A��!A�33A�  A��A�&�A��A�O�A���A���A��uA�7LA��A��^A�^5A�&�A�1A��A�x�A���A�?}A�1A�  A�n�A���A��uA�XA��!A���A�I�A���A�%A�9XA���A�O�A���A�l�A��A��wA���A�~�A�&�A��A���A�Q�A���A��HA�l�A���A���A��uA�1A�E�A�z�A�M�A�(�A���A��A�M�A�?}A� �A�=qA�{A�oA���A�`BA��A��A�ƨA���A��A~��A|��AwO�As�^Ap�/Aj�HAe�PAeO�Ad��A`ȴA_��A\�HA\��A\�DA[�mA[l�AZ��AZ�`AZ$�AXz�AVjAV�9AVZAUO�AS�AShsAR��AR�yAR��AQ�AO�7AN{ALJAK33AJ�AJr�AI�AIO�AH��AHr�ADAC�
AC"�AAl�A?A>z�A>�!A=��A9A7�#A7�hA7G�A6�\A6(�A5�7A3�wA2�A2�/A2(�A1%A/��A.��A-��A-A-�A,�\A,1A+dZA+
=A*z�A*1'A*1'A)��A)
=A'��A&�HA&^5A%�#A%t�A$��A$�uA$=qA"�HA!�A!?}AhsAȴA��AffA  A$�A1'A&�Az�AAC�AbNA"�A�A�RA
=AȴA
(�Az�AA��A+AQ�A�FA�hA �A&�A ��A �@�;d@��!@�-@���@��/@� �@��@�o@���@�dZ@�@�33@���@홚@�dZ@�Ĝ@旍@�?}@⟾@�O�@��m@ߕ�@߅@�;d@�@��@�@���@�~�@�%@�b@ۥ�@ڟ�@��@�Z@׶F@���@��H@��@�9X@�r�@���@��/@��H@�^5@��^@���@�b@��@�n�@�5?@��@���@��7@�?}@���@��u@�9X@��F@�5?@�I�@���@�o@�ȴ@���@���@�v�@�ff@�=q@�$�@�p�@��;@�|�@�@��@��@��/@��u@�Q�@�1@��
@���@�|�@�C�@���@��#@���@���@��@��@��7@�G�@�7L@��/@��F@�33@�C�@�5?@��7@�hs@�hs@�`B@�x�@��-@�@���@��T@��@���@�$�@�v�@��R@�@�t�@�|�@��@��y@��\@�=q@�M�@�^5@��+@�ȴ@��y@���@��R@�@�o@�@���@�v�@��@�9X@��F@��
@��P@���@��\@�5?@�E�@�V@���@��@��@�|�@�b@�j@�bN@�  @��@�ƨ@���@�|�@��@�
=@��@�J@�x�@�&�@���@���@�1'@�  @��w@��@�C�@��y@��@��@�ȴ@���@�ȴ@���@�^5@�/@���@�z�@���@��@�E�@�@�33@���@��-@��T@�5?@��@���@��/@�j@�A�@�  @��j@�G�@��#@�5?@��-@��u@�ƨ@��;@�(�@�9X@�Z@���@�Ĝ@�%@�V@���@�Ĝ@��j@�j@���@�\)@�+@��@�=q@��@�\)@�@��7@�V@��@�z�@�r�@��u@��`@���@�V@�7L@�x�@�7L@��/@���@�Ĝ@��@�9X@��;@�|�@�dZ@�C�@��y@���@�ff@�@���@�x�@�hs@�`B@�hs@�7L@��`@��D@�z�@��u@�z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBw�Bx�B{�By�Bz�Bz�By�Bz�Bz�Bz�Bz�Bz�B{�B|�B|�B|�B|�B|�B|�B{�B|�B}�B}�B}�B}�B}�B}�B}�B}�B}�B}�B}�B}�B}�B}�B}�B}�Bx�Bp�BffB|�B{�B|�Bn�Bl�BbNBJ�B?}B33B)�BaHB�uB�B��B�B�B�'B�'B�B�B�B��B��B��B��B��B��B��B��B�VB�1B�B�B~�B|�B}�Bz�Bt�BgmB`BBW
BR�BT�BQ�BF�B:^B-B�B+B��B�fB�#B��B��B��B�7Bv�B`BBC�B,B �B�B�B�B�BbB  B
�ZB
��B
�}B
�B
��B
�B
n�B
l�B
jB
gmB
`BB
Q�B
7LB
1B	�sB	�B	�?B	��B	��B	�uB	�%B	{�B	ffB	cTB	bNB	\)B	W
B	S�B	VB	R�B	G�B	@�B	H�B	G�B	B�B	>wB	=qB	8RB	7LB	5?B	0!B	!�B	�B	bB		7B	+B	+B	B	B��B��B�B�B�B�)BŢBÖBƨB��B��B�B�B�B��B��B��B��B��B��B��B��B��B�{B�uB�uB�hB�bB�\B�\B�bB�oB��B��B��B�{B�{B��B�{B�uB�hB�\B�PB�DB�1B�7B�%B�B� B~�B|�Bw�Bp�Bl�BjBiyBgmBffBcTBaHBaHB^5B[#BZB]/B^5B^5B]/B\)BZBXBXBVBVBT�BS�BS�BS�BS�BR�BR�BR�BP�BN�BL�BK�BJ�BJ�BI�BG�BH�BD�BD�BB�BI�BM�BQ�BR�BS�BS�BT�BS�BS�BS�BR�BT�BT�BT�BT�BW
BYBXBXB`BBgmBl�Br�Bu�Bw�B{�B{�B|�B~�B� B~�B� B�B�B�B�B�B�B�+B�1B�=B�VB�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B�!B�LB�^B�jB�qB�wB�}B�wB�}B��B��BĜBĜBĜBŢBŢBŢBƨBǮB��B��B�5B�fB�B��B��B��B��B��B	  B	B	B	B	B	%B	1B	DB	PB	hB	�B	�B	�B	#�B	'�B	-B	/B	0!B	5?B	7LB	9XB	:^B	;dB	>wB	?}B	B�B	D�B	E�B	F�B	D�B	D�B	E�B	D�B	A�B	@�B	B�B	I�B	K�B	O�B	R�B	W
B	]/B	dZB	gmB	n�B	s�B	v�B	y�B	{�B	|�B	� B	� B	� B	�B	�1B	�=B	�DB	�PB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�PB	�DB	�PB	�\B	�oB	��B	��B	�oB	�hB	�hB	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�?B	�LB	�^B	�qB	�wB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	ÖB	B	B	��B	��B	ÖB	ƨB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�)B	�)B	�5B	�;B	�BB	�BB	�NB	�NB	�NB	�NB	�TB	�NB	�NB	�TB	�`B	�sB	�s1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bw�Bx�B{�By�Bz�Bz�By�Bz�Bz�Bz�Bz�Bz�B{�B|�B|�B|�B|�B|�B|�B{�B|�B}�B}�B}�B}�B}�B}�B}�B}�B}�B}�B}�B}�B}�B}�B}�B� B� Bw�Bo�B�%B~�B�Bq�Br�BiyBM�BA�B<jB+BaHB��B�?B�B�B�!B�-B�3B�'B�B�B��B��B��B��B��B��B��B��B��B�DB�B�B�B�B� B}�Bw�Bk�BffBZBVBW
BVBJ�B?}B2-B#�BPB��B�yB�5B��BɺB�'B�VB}�BiyBL�B0!B!�B�B�B�B�B{B1B
�sB
�B
ĜB
�3B
��B
�7B
o�B
m�B
k�B
jB
dZB
XB
E�B
hB	�B	�B	ĜB	��B	��B	��B	�=B	�B	gmB	dZB	dZB	^5B	YB	T�B	YB	YB	N�B	?}B	J�B	K�B	F�B	@�B	?}B	8RB	8RB	8RB	7LB	%�B	!�B	oB	
=B	1B	1B	%B	B��B	  B�
B�B�B�NBƨBÖBɺB��B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�{B��B�uB�oB�hB�bB�oB�uB��B��B��B��B��B��B��B��B�uB�hB�\B�bB�JB�JB�JB�B�B� B� B�%Bw�Bp�Bm�Bk�BjBjBhsBgmBdZBdZBcTBcTBbNB_;B_;B^5B^5B\)B]/B\)BYBW
BW
BVBT�BT�BT�BS�BS�BS�BR�BT�BT�BP�BM�BK�BK�BK�BM�BH�BG�BG�BL�BP�BR�BR�BT�BT�BT�BS�BT�BT�BVBW
BVBW
BXBXBYB[#B_;BhsBl�Bq�Bu�Bw�Bz�B|�B|�B}�B� B�B� B� B�B�B�B�B�B�%B�1B�7B�PB�hB�hB�uB��B��B��B��B��B��B��B��B��B��B��B�B�-B�RB�dB�qB�wB�wB�}B�}B��B��BBĜBĜBĜBŢBŢBŢBƨBȴB��B��B�5B�mB�B��B��B��B��B��B	  B	B	B	B	B	%B	1B	DB	PB	bB	�B	�B	�B	$�B	(�B	-B	/B	0!B	5?B	7LB	9XB	:^B	;dB	>wB	?}B	C�B	E�B	F�B	I�B	E�B	D�B	F�B	F�B	H�B	A�B	B�B	I�B	K�B	O�B	R�B	VB	\)B	dZB	gmB	o�B	s�B	v�B	y�B	{�B	}�B	� B	� B	�B	�B	�7B	�=B	�JB	�VB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�\B	�DB	�PB	�\B	�hB	��B	��B	�uB	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	�B	�3B	�?B	�LB	�^B	�qB	�wB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ĜB	ÖB	ÖB	��B	��B	ÖB	ƨB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�/B	�;B	�BB	�HB	�BB	�TB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�`B	�sB	�s1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<�o<e`B<#�
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
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447112012010314471120120103144711  AO  ARGQ                                                                        20111130141608  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141608  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144711  IP                  G�O�G�O�G�O�                