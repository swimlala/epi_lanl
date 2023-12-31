CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:18Z UW 3.1 conversion   
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
_FillValue                 �  Kl   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mh   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  UP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �\   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �l   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               cA   AO  20111130141416  20190522121826  1727_5046_099                   2C  D   APEX                            2143                            040306                          846 @ԕ��Ŀ�1   @ԕ�b�� @6�� ě��c�=p��
1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0fD0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy�3D�,�D�l�D���D���D�6fD�l�D��fD���D�  D�s3D�� D��3D�,�D�Y�Dڣ3D��D�  D�` D� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0fD0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy�3D�,�D�l�D���D���D�6fD�l�D��fD���D�  D�s3D�� D��3D�,�D�Y�Dڣ3D��D�  D�` D� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1'A�33A�5?A�;dA�=qA�?}A�?}A�?}A�A�A�A�A�A�A�A�A�C�A�E�A�E�A�E�A�=qA�;dA�;dA�7LA�5?A�33A�1'A�(�A�+A�oA�{A���A�M�A��A�dZA�I�A���A�-A�bAŋDA�
=A�1A�bA���A�A�A��A�$�A�;dA��A�/A��A��PA�M�A�1A���A�ȴA��A�ffA��TA�7LA�oA��`A��\A�{A��!A�-A�x�A�/A��^A�~�A�bA���A�\)A���A���A�"�A��A�XA�z�A�p�A�&�A�VA��A��`A�p�A�(�A��;A��A��A��hA�E�A��FA���A���A�bNA�$�A�VA��7A���A���A��jA��A��A���A�A�A�ȴA�K�A�E�A��9A��A�9XA��A�A�O�A�A�n�A�hsA�dZA��A�5?A���A�oA���A�M�A���A���A�E�A�1'A��uA�bA�bNA�JA�Q�A��A���AS�A|M�Ayp�Aw�AtȴArE�Ap��Ao?}Anz�AlffAjn�AhE�Ag�-AfA�Ad�Ac�
Ac�Ab^5Aal�A_��A^�!A\��AZ�yAXVAVz�ASƨAQ��AP��AOANA�AMK�AKx�AJ9XAI��AH��AH(�AGXAD��AChsA?33A;��A:��A:Q�A9��A8�A7��A6�RA65?A5�;A5�#A57LA4�uA3A1�
A0ffA//A.��A-��A-x�A-
=A+�
A+;dA*1'A(��A(1A&�A%�
A%\)A$�+A$$�A#�#A#�A#dZA#+A"v�A!dZA �`A   AȴA�mAhsAffA5?AbA�A�^AK�A�;A��AE�A��AJA�FAȴAG�AZA(�A$�A�mAoA~�A33AJAbNA\)A	�
Ar�A�A�9AoA�-A�AG�AA��AhsA �uA 5?@�C�@��@���@�A�@���@�C�@�-@�hs@���@���@��@�-@�K�@���@�9X@�-@�O�@�9@�9X@�t�@旍@���@�r�@�=q@��u@ܴ9@٩�@�r�@�+@��`@��H@�`B@�&�@У�@��;@ΰ!@̃@�ff@�;d@��H@ƸR@�@�X@�r�@���@�~�@�  @�5?@��h@���@�;d@���@�E�@���@���@�X@�z�@�z�@���@�(�@���@���@�dZ@�
=@���@�@�C�@��@���@�
=@���@�\)@�^5@�5?@���@�V@�1'@��@��@�Ĝ@��@�$�@��9@�O�@�7L@�V@��j@�v�@�
=@���@��h@��@��9@���@�K�@��@��@�ȴ@���@���@�I�@�(�@�b@��m@�ƨ@��@�+@�o@�t�@��F@��;@��
@��F@��@�\)@�K�@�C�@�;d@�+@�o@��H@�^5@��@��7@�O�@�V@��j@�I�@�ƨ@���@���@���@���@��w@���@�=q@���@��7@�G�@���@��@�K�@���@�ȴ@��@��y@��y@��@��@��@��H@��R@���@�@��7@�?}@�/@�%@��9@�j@�1'@�  @��;@���@��w@��w@��@�C�@���@�V@��@�p�@�&�@��@�r�@� �@���@��m@��
@��w@��F@�;d@���@���@��!@�5?@��h@�x�@�`B@�`B@��7@�hs@�G�@���@��/@���@���@��@�x�@��^@��^@�@�@�@���@���@��7@�p�@�hs@�O�@�X@�p�@�p�@�G�@�7L@�/@��@��@�V@���@��/@���@�Ĝ@��@��u@�b@��w@���@��@�|�@�t�@�l�@�K�@�"�@��y@���@���@�1@xbN@pbN@l��@b��@^E�@W;d@Kƨ@D�/@>ff@6{@1��@,Z@'��@#��@\)@��@j@�R@
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�1'A�33A�5?A�;dA�=qA�?}A�?}A�?}A�A�A�A�A�A�A�A�A�C�A�E�A�E�A�E�A�=qA�;dA�;dA�7LA�5?A�33A�1'A�(�A�+A�oA�{A���A�M�A��A�dZA�I�A���A�-A�bAŋDA�
=A�1A�bA���A�A�A��A�$�A�;dA��A�/A��A��PA�M�A�1A���A�ȴA��A�ffA��TA�7LA�oA��`A��\A�{A��!A�-A�x�A�/A��^A�~�A�bA���A�\)A���A���A�"�A��A�XA�z�A�p�A�&�A�VA��A��`A�p�A�(�A��;A��A��A��hA�E�A��FA���A���A�bNA�$�A�VA��7A���A���A��jA��A��A���A�A�A�ȴA�K�A�E�A��9A��A�9XA��A�A�O�A�A�n�A�hsA�dZA��A�5?A���A�oA���A�M�A���A���A�E�A�1'A��uA�bA�bNA�JA�Q�A��A���AS�A|M�Ayp�Aw�AtȴArE�Ap��Ao?}Anz�AlffAjn�AhE�Ag�-AfA�Ad�Ac�
Ac�Ab^5Aal�A_��A^�!A\��AZ�yAXVAVz�ASƨAQ��AP��AOANA�AMK�AKx�AJ9XAI��AH��AH(�AGXAD��AChsA?33A;��A:��A:Q�A9��A8�A7��A6�RA65?A5�;A5�#A57LA4�uA3A1�
A0ffA//A.��A-��A-x�A-
=A+�
A+;dA*1'A(��A(1A&�A%�
A%\)A$�+A$$�A#�#A#�A#dZA#+A"v�A!dZA �`A   AȴA�mAhsAffA5?AbA�A�^AK�A�;A��AE�A��AJA�FAȴAG�AZA(�A$�A�mAoA~�A33AJAbNA\)A	�
Ar�A�A�9AoA�-A�AG�AA��AhsA �uA 5?@�C�@��@���@�A�@���@�C�@�-@�hs@���@���@��@�-@�K�@���@�9X@�-@�O�@�9@�9X@�t�@旍@���@�r�@�=q@��u@ܴ9@٩�@�r�@�+@��`@��H@�`B@�&�@У�@��;@ΰ!@̃@�ff@�;d@��H@ƸR@�@�X@�r�@���@�~�@�  @�5?@��h@���@�;d@���@�E�@���@���@�X@�z�@�z�@���@�(�@���@���@�dZ@�
=@���@�@�C�@��@���@�
=@���@�\)@�^5@�5?@���@�V@�1'@��@��@�Ĝ@��@�$�@��9@�O�@�7L@�V@��j@�v�@�
=@���@��h@��@��9@���@�K�@��@��@�ȴ@���@���@�I�@�(�@�b@��m@�ƨ@��@�+@�o@�t�@��F@��;@��
@��F@��@�\)@�K�@�C�@�;d@�+@�o@��H@�^5@��@��7@�O�@�V@��j@�I�@�ƨ@���@���@���@���@��w@���@�=q@���@��7@�G�@���@��@�K�@���@�ȴ@��@��y@��y@��@��@��@��H@��R@���@�@��7@�?}@�/@�%@��9@�j@�1'@�  @��;@���@��w@��w@��@�C�@���@�V@��@�p�@�&�@��@�r�@� �@���@��m@��
@��w@��F@�;d@���@���@��!@�5?@��h@�x�@�`B@�`B@��7@�hs@�G�@���@��/@���@���@��@�x�@��^@��^@�@�@�@���@���@��7@�p�@�hs@�O�@�X@�p�@�p�@�G�@�7L@�/@��@��@�V@���@��/@���@�Ĝ@��@��u@�b@��w@���@��@�|�@�t�@�l�@�K�@�"�@��y@���@���@�1@xbN@pbN@l��@b��@^E�@W;d@Kƨ@D�/@>ff@6{@1��@,Z@'��@#��@\)@��@j@�R@
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�mB�mB�mB�fB�mB�mB�mB�mB�mB�mB�mB�mB�fB�fB�mB�fB�`B�ZB�`B�`B�ZB�ZB�TB�NB�NB�;B��B�9BB�5B��B�B�BƨB�\B�B�B��B�3B�?B�jB��B�`B��B�B)�B.B7LB:^B?}BF�BL�BVB[#Bm�Bt�Bs�Bs�Bv�B}�B�B�1B�oB��B��B��B��B��B��B��B��B�B�3B�9B�-B��B��B�!B�!B�B��B�+B|�Bw�Bu�Bn�Bo�Bk�BjBiyBcTBT�BE�B;dB.B �B�B�B�B�BoB1B�B��BǮB�XB��B�1Bx�Bk�BVBH�B<jB'�B%B
��B
�B
�yB
�yB
�sB
�fB
�TB
�#B
��B
�LB
��B
��B
�{B
�B
z�B
k�B
XB
D�B
1'B
#�B
VB
  B	��B	�B	�mB	�)B	��B	��B	�dB	�B	��B	��B	��B	��B	��B	��B	�VB	�B	}�B	s�B	dZB	YB	T�B	R�B	J�B	?}B	;dB	>wB	33B	33B	:^B	>wB	49B	"�B	oB�sBĜB��B�wB�dB�XB�dB�^B�RB�RB�RB�LB�RB�3B��B��B��B��B��B��B��B�{B�uB�hB�hB�\B�VB�VB�hB�\B�VB�PB�JB�DB�JB�1B�%B�B�B�B�B� B� B� B� B~�B|�By�Bx�Bu�Br�Bs�Bs�Bq�Bn�Bl�BjBk�Bk�BiyBiyBgmBffBbNB]/BXBR�BT�B^5BZBS�BT�BVBT�BS�BQ�BO�BK�BM�BJ�BJ�BM�BM�BL�BK�BL�BK�BJ�BH�BH�BF�BF�BE�BC�BB�BC�BB�BA�BC�BD�BC�BE�BG�BE�B<jB<jB=qB9XB7LB7LB8RB8RB6FB6FB5?B5?B33B8RB;dB:^B:^B;dB<jB<jB;dB=qB@�B?}BD�BG�BK�BM�BM�BM�BO�BZBgmBs�B�=B�JB�JB�DB�7B�B� B�DB��B��B��B��B�VB�hB�oB�hB�\B�VB�VB�oB�{B�\B�\B�oB��B��B��B�{B�oB�\B�oB�oB�uB��B��B��B��B��B��B�B�3B�?B�FB�LB�XB�dB�wBBǮB��B�B�/B�HB�fB�B�B�B��B��B��B��B��B	B	%B	+B	1B		7B	
=B	hB	oB	uB	hB	hB	bB	bB	uB	�B	�B	�B	�B	�B	#�B	'�B	,B	/B	2-B	5?B	6FB	7LB	7LB	6FB	6FB	6FB	9XB	:^B	:^B	=qB	=qB	?}B	A�B	D�B	E�B	F�B	G�B	G�B	G�B	G�B	G�B	J�B	N�B	P�B	S�B	W
B	ZB	]/B	dZB	hsB	jB	k�B	l�B	m�B	n�B	v�B	z�B	{�B	|�B	}�B	� B	�B	�B	�B	�DB	�JB	�PB	�VB	�VB	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�-B	�9B	�RB	�dB	�jB	�qB	�qB	�qB	�qB	�wB	�}B	��B	��B	ÖB	��B	�mB	��B
1B
�B
"�B
&�B
/B
5?B
;dB
D�B
H�B
N�B
R�B
W
B
]/B
dZB
iyB
p�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�mB�mB�mB�fB�mB�mB�mB�mB�mB�mB�mB�mB�fB�fB�mB�fB�`B�ZB�`B�`B�ZB�ZB�TB�NB�TB�fBǮB�?B
=B�BB��B�B$�B��B�hB�+B�JB��B�9B�LB��B�B�yB��B�B+B0!B8RB;dBA�BI�BN�BXB`BBo�Bu�Bt�Bt�Bx�B� B�B�DB�uB��B��B��B��B��B��B�B�B�B�9B�RB�-B��BŢB�9B�'B�B��B�PB~�By�Bx�Bu�Br�Bk�BjBl�Bq�B\)BI�BA�B33B!�B�B�B�B�B�BoB��B�B��BÖB��B�JB{�Bq�B\)BL�BA�B33B
=BB
��B
�B
�B
�B
�mB
�`B
�TB
��B
�qB
��B
��B
��B
�7B
}�B
q�B
`BB
L�B
6FB
,B
�B
B	��B	�B	�B	�HB	��B	B	�}B	�3B	�B	��B	��B	��B	��B	��B	�uB	�=B	�%B	y�B	l�B	_;B	YB	VB	O�B	C�B	A�B	B�B	5?B	6FB	=qB	B�B	=qB	(�B	!�B��BǮBB��B�}B�qB�wB�jB�XB�RB�^B�XB�^B�LB�B��B��B��B��B��B��B��B��B��B�uB�uB�hB�bB�{B�bB�\B�VB�PB�JB�\B�JB�1B�+B�%B�B�B�B�B�B�B� B~�B~�By�Bz�Bw�Bu�Bt�Bt�Bs�Bo�Bk�Bk�Bl�Bl�Bk�Bk�BjBgmBaHB]/BW
BW
BbNB^5BW
BVBW
BVBT�BT�BQ�BL�BO�BO�BN�BN�BN�BM�BM�BM�BL�BL�BJ�BJ�BJ�BI�BH�BF�BC�BD�BC�BB�BE�BE�BE�BI�BJ�BK�BA�B>wB?}B=qB:^B9XB9XB9XB7LB8RB9XB9XB8RB9XB<jB;dB;dB=qB=qB?}B?}B?}BA�BA�BE�BH�BL�BN�BM�BN�BP�BZBffBp�B�=B�PB�JB�JB�DB�1B� B�=B��B��B��B��B�bB�hB�uB�uB�hB�VB�PB�uB��B�hB�hB�hB��B��B��B��B��B�hB�oB�oB�{B��B��B��B��B��B��B�B�9B�?B�FB�LB�XB�dB�wBBǮB��B�B�/B�HB�fB�B�B�B��B��B��B��B��B	B	+B	1B		7B	
=B	DB	oB	uB	�B	uB	uB	oB	oB	{B	�B	�B	�B	�B	 �B	$�B	(�B	,B	/B	2-B	5?B	6FB	7LB	7LB	6FB	6FB	7LB	9XB	:^B	;dB	=qB	=qB	@�B	B�B	D�B	E�B	F�B	G�B	G�B	G�B	G�B	H�B	K�B	O�B	Q�B	T�B	W
B	ZB	^5B	dZB	hsB	jB	k�B	l�B	m�B	o�B	w�B	z�B	{�B	}�B	~�B	� B	�B	�B	�B	�DB	�JB	�VB	�VB	�VB	�VB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�-B	�?B	�XB	�dB	�jB	�qB	�qB	�qB	�qB	�wB	�}B	��B	��B	ÖB	��B	�mB	��B
1B
�B
"�B
&�B
/B
5?B
;dB
D�B
H�B
N�B
R�B
W
B
]/B
dZB
iyB
p�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<u<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447082012010314470820120103144708  AO  ARGQ                                                                        20111130141416  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141416  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144708  IP                  G�O�G�O�G�O�                